use std::{
    fmt::Display,
    path::{Path, PathBuf},
    rc::Rc,
};

use colored::Color;
use memoize::memoize;
use reqwest::Url;

use crate::utils::{cli_label, Rcable};

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ModuleID {
    Local(Rc<PathBuf>),
    Remote(Rc<Url>),
    Npm(Rc<String>),
    Artificial(Rc<String>),
}

impl ModuleID {
    pub fn load(&self, clean: bool) -> Option<String> {
        match self {
            ModuleID::Local(path) => std::fs::read_to_string(path.as_ref()).ok(),
            ModuleID::Remote(url) => {
                let cache_dir = cache_dir();
                let cache_path = cache_dir.as_ref().map(|base_dir| {
                    base_dir.join(url_escape::encode_component(url.as_str()).to_string() + ".bgl")
                });

                if let Some(cache_dir) = cache_dir {
                    if !cache_dir.exists() {
                        std::fs::create_dir_all(cache_dir).unwrap();
                    }
                }

                if !clean {
                    if let Some(cache_path) = &cache_path {
                        if let Some(cached) = std::fs::read_to_string(cache_path).ok() {
                            return Some(cached);
                        }
                    }
                }

                let loaded = reqwest::blocking::get(url.as_ref().clone())
                    .ok()
                    .filter(|res| res.status().is_success())
                    .map(|res| res.text().ok())
                    .flatten();

                if let Some(loaded) = loaded {
                    println!("{} {}", cli_label("Downloaded", Color::Green), url.as_str());

                    if let Some(cache_path) = &cache_path {
                        let write_res = std::fs::write(cache_path, &loaded);

                        if write_res.is_err() {
                            println!(
                                "{} failed writing cache of module {}",
                                cli_label("Warning", Color::Yellow),
                                url.as_str()
                            );
                        }
                    }

                    return Some(loaded);
                } else {
                    println!(
                        "{} couldn't load remote module {}",
                        cli_label("Error", Color::Red),
                        url.as_str()
                    );
                }

                None
            }
            ModuleID::Npm(_) => None,
            ModuleID::Artificial(_) => unreachable!(),
        }
        .map(|raw_string| {
            if raw_string.contains('\r') {
                raw_string.chars().filter(|c| *c != '\r').collect()
            } else {
                raw_string
            }
        })
    }

    pub fn imported(&self, imported: &str) -> Option<ModuleID> {
        if imported.starts_with("https://") || imported.starts_with("http://") {
            Url::parse(imported).ok().map(ModuleID::from)
        } else if imported.starts_with("/") {
            ModuleID::try_from(Path::new(imported)).ok()
        } else if imported.starts_with("npm:") {
            Some(ModuleID::Npm(imported.to_owned().rc()))
        } else {
            match self {
                ModuleID::Local(this) => this
                    .parent()
                    .map(|dir| dir.to_path_buf().join(imported).canonicalize().ok())
                    .flatten()
                    .map(|path| ModuleID::try_from(path.as_path()).ok())
                    .flatten(),
                ModuleID::Remote(this) => this.join(imported).ok().map(ModuleID::from),
                ModuleID::Npm(this) => Some(ModuleID::Npm(this.clone())),
                ModuleID::Artificial(_) => Some(ModuleID::Artificial(imported.to_owned().rc())),
            }
        }
    }

    pub fn is_std_lib(&self) -> bool {
        match self {
            ModuleID::Local(path) => path
                .to_string_lossy()
                .starts_with("/Users/brundolf/git/bagel-rs/lib/bgl"),
            ModuleID::Remote(url) => url.as_str().starts_with(
                "https://raw.githubusercontent.com/brundonsmith/bagel-rs/master/lib/bgl",
            ),
            ModuleID::Npm(_) => false,
            ModuleID::Artificial(_) => false,
        }
    }
}

impl Display for ModuleID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModuleID::Local(p) => f.write_str(&p.to_string_lossy()),
            ModuleID::Remote(s) => f.write_str(s.as_str()),
            ModuleID::Npm(s) => f.write_str(s.as_str()),
            ModuleID::Artificial(s) => f.write_str(s.as_str()),
        }
    }
}

impl TryFrom<&Path> for ModuleID {
    type Error = std::io::Error;

    fn try_from(p: &Path) -> Result<ModuleID, std::io::Error> {
        Ok(ModuleID::Local(Rc::new(p.canonicalize()?)))
    }
}

impl From<Url> for ModuleID {
    fn from(s: Url) -> Self {
        Self::Remote(Rc::new(s))
    }
}

#[memoize]
fn cache_dir() -> Option<PathBuf> {
    match std::env::consts::OS {
        "macos" => std::env::var("HOME")
            .map(|home_dir| PathBuf::from(home_dir).join("Library/Caches"))
            .ok(),
        "windows" => std::env::var("LOCALAPPDATA")
            .map(PathBuf::from)
            .or(std::env::var("USERPROFILE")
                .map(PathBuf::from)
                .map(|dir| dir.join("AppData/Local")))
            .map(|base_dir| base_dir.join("Cache"))
            .ok(),
        "linux" => std::env::var("XDG_CACHE_HOME").map(PathBuf::from).ok(),
        _ => None,
    }
    .map(|base_dir| base_dir.join("bagel"))
}
