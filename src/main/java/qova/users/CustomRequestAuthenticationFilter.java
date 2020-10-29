package qova.users;

import org.springframework.security.web.authentication.preauth.AbstractPreAuthenticatedProcessingFilter;

import javax.servlet.http.HttpServletRequest;

public class CustomRequestAuthenticationFilter extends AbstractPreAuthenticatedProcessingFilter {


    @Override
    protected Object getPreAuthenticatedPrincipal(HttpServletRequest request) {
        return request.getAttribute("persistent-id");
    }

    @Override
    protected Object getPreAuthenticatedCredentials(HttpServletRequest request) {
        return request.getAttribute("affiliation");
    }
}
