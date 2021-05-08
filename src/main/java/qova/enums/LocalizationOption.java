package qova.enums;

/**
 * <p>Used to distinguish between which languge to display to the user</p>
 * <p>In {@linkplain qova.QovaApplication} the Locale is discerned using LocalInterceptor, which sets the language based on the httpHeader sent initially by
 * the browser, then sets a cookie. In theory one can change locale by setting the cookie to another language, but this functionality failed to work in production.</p>
 */
public enum LocalizationOption {
    EN, DE;
    
}