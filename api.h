/**
 * Represents an instance of a Mun environment. Although technically it is
 * possible to instantiate multiple Mun environments one is in general all you
 * need.
 * @see `mun_init`
 */
typedef void* MunEnvironment;

/**
 * A `MunEnvironmentConfig` contains initialization properties for a Mun
 * configuration.
 */
struct MunEnvironmentConfig {
  // TODO: Add memory allocation methods
};

/**
 * Initializes a Mun environment. You can initialize multiple Mun environments
 * but one is all you need really.
 * @param[in] config A pointer to a Mun environment configuration or 0 to use
 *  the defaults.
 */
MunEnvironment mun_init(MunEnvironmentConfig *config);

/**
 * Releases all resources associated with the specified Mun envionment.
 * @param[in] env The environment to destroy.
 */
void mun_destroy(MunEnvironment env);