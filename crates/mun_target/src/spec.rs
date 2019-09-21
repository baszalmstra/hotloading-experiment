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

    /// Default CPU to pass to LLVM. Corresponds to `llc -mcpu=$cpu`. Defaults to "generic".
    pub cpu: String,

    /// Default target features to pass to LLVM. These features will *always* be passed, and cannot
    /// be disabled even via `-C`. Corresponds to `llc -mattr=$features`.
    pub features: String,
}

impl Default for TargetOptions {
    fn default() -> Self {
        TargetOptions {
            is_builtin: false,
            cpu: "generic".to_string(),
            features: "".to_string(),
        }
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

supported_targets!(("x86_64-apple-darwin", x86_64_apple_darwin),);

impl Target {
    pub fn search(target_triple: &str) -> Result<Target, String> {
        match load_specific(target_triple) {
            Ok(t) => Ok(t),
            Err(LoadTargetError::BuiltinTargetNotFound(_)) => Err(format!(
                "Could not find specification for target {:?}",
                target_triple
            )),
            Err(LoadTargetError::Other(e)) => Err(e),
        }
    }
}
