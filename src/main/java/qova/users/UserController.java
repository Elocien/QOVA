package qova.users;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.ResponseBody;
import qova.logic.CourseManagement;


import javax.servlet.http.HttpServletRequest;
import java.util.Objects;

@Controller
public class UserController {

    private final UserManagement userManagement;

    @Autowired
    UserController(UserManagement userManagement) {
        this.userManagement = Objects.requireNonNull(userManagement);
    }

    @GetMapping("/secure")
    @ResponseBody
    public String authenticate(HttpServletRequest request){
        String unscopedAffiliation = request.getHeader("AJP_unscoped-affiliation");


        return unscopedAffiliation;
    }
}
