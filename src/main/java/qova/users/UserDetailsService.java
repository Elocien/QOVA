package qova.users;

import org.springframework.security.core.userdetails.AuthenticationUserDetailsService;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.security.web.authentication.preauth.PreAuthenticatedAuthenticationToken;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

@Component
public class UserDetailsService implements
        AuthenticationUserDetailsService<PreAuthenticatedAuthenticationToken>{

    final
    UserRepository userRepository;

    public UserDetailsService(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    @Override
    public UserDetails loadUserDetails(PreAuthenticatedAuthenticationToken preAuthenticatedAuthenticationToken) throws UsernameNotFoundException {

        //AJP_persistent-id
        String persistentId = preAuthenticatedAuthenticationToken.getPrincipal().toString();

        //Exception if empty
        if (persistentId.isEmpty()) {
            throw new UsernameNotFoundException("Username is empty");
        }

        //Retrieve User
        Optional<User> user = userRepository.findByAjpPersistentId(persistentId);
        if (user.isPresent()) {

            //Return UserDetails Object
            return user.get().toCurrentUserDetails();

        } else {

            //Retrieve User role
            String userRole = getUserRoleFromCredentials(preAuthenticatedAuthenticationToken.getCredentials().toString());

            //Create new user
            User newUser = new User(persistentId, userRole);

            //Save the User
            userRepository.save(newUser);

            return newUser.toCurrentUserDetails();
        }
    }


    /**
     * Retrieve the user role from the AJP_affiliation.
     * <a href="https://doku.tid.dfn.de/de:common_attributes">shibboleth attributes</a>
     *
     *
     * @param credentials The Credentials field from the {@linkplain PreAuthenticatedAuthenticationToken}
     * @return The userRole extracted from the {@linkplain PreAuthenticatedAuthenticationToken} Credentials
     */
    private String getUserRoleFromCredentials(String credentials) {
        List<String> roles = Arrays.asList(credentials.split(";"));
        if (roles.contains("staff@tu-dresden.de") || roles.contains("employee@tu-dresden.de") || roles.contains("faculty@tu-dresden.de") || roles.contains("affiliate@tu-dresden.de")) {
            return "STAFF";
        } else if (roles.contains("student@tu-dresden.de") || roles.contains("member@tu-dresden.de") || roles.contains("alum@tu-dresden.de")) {
            return "STUDENT";
        } else {
            return "VISITOR";
        }
    }
}
