package qova.users;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.header.Header;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ResponseBody;


import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Controller
public class UserController {

    private final UserManagement userManagement;

    @Autowired
    UserController(UserManagement userManagement) {
        this.userManagement = Objects.requireNonNull(userManagement);
    }

    @GetMapping("/secure/principal")
    @ResponseBody
    public String currentUserPrincipal(Authentication authentication) {
        return authentication.getPrincipal().toString();
    }

    @GetMapping("/secure/credentials")
    @ResponseBody
    public String currentUserCedentials(Authentication authentication) {
        return authentication.getCredentials().toString();
    }

}
