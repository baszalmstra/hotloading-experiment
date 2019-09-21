mod apple_base;

/// Everything Mun knows a bout a target.
/// Every field must be specified, there are no default values.
#[derive(PartialEq, Clone, Debug)]
pub struct Target {
    /// Target triple to pass to LLVM
    pub llvm_target: String,

    /// The name of the OS
    pub target_os: String,

    /// The name of the environment
    pub target_env: String,

    /// The name of the vendor
    pub target_vendor: String,

    /// The name of the architecture. For example "x86" or "x86_64"
    pub arch: String,

    /// Optional settings
    pub options: TargetOptions,
}

/// Optional aspects of target specification.
#[derive(PartialEq, Clone, Debug)]
pub struct TargetOptions {
    /// True if this is a built-in target
    pub is_builtin: bool,
}

impl Default for TargetOptions {
    fn default() -> Self {
        TargetOptions { is_builtin: false }
    }
}

pub enum LoadTargetError {
    BuiltinTargetNotFound(String),
    Other(String),
}

pub type TargetResult = Result<Target, String>;

macro_rules! supported_targets {
    ( $(($( $triple:literal, )+ $module:ident ),)+ ) => {
        $ ( mod $ module; ) +

        /// List of supported targets
        const TARGETS: &[&str] = &[$($($triple),+),+];

        fn load_specific(target: &str) -> Result<Target, LoadTargetError> {
            match target {
                $(
                    $($triple)|+ => {
                        let mut t = $module::target()
                            .map_err(LoadTargetError::Other)?;
                        t.options.is_builtin = true;

                        debug!("got builtin target: {:?}", t);
                        Ok(t)
                    },
                )+
                    _ => Err(LoadTargetError::BuiltinTargetNotFound(
                        format!("Unable to find target: {}", target)))
            }
        }

        pub fn get_targets() -> impl Iterator<Item = String> {
            TARGETS.iter().filter_map(|t| -> Option<String> {
                load_specific(t)
                    .and(Ok(t.to_string()))
                    .ok()
            })
        }
    }
}

supported_targets!(("x86_64_apple_darwin", x86_64_apple_darwin),);
