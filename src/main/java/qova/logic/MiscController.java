package qova.logic;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.ResourceUtils;
import org.springframework.web.bind.annotation.GetMapping;
import qova.users.CurrentUserDetails;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Objects;

@Controller
public class MiscController {

    @Autowired
    private final CourseManagement courseManagement;

    @Autowired
    MiscController(CourseManagement courseManagement){
        this.courseManagement = Objects.requireNonNull(courseManagement);
    }

    /**
     * Landing Page for the application
     * @return home.html template
     */
    @GetMapping("/")
    public String welcome(Model model, @AuthenticationPrincipal CurrentUserDetails userDetails, HttpServletRequest request) {

        //If user is logged in, the username will be present and not null
        if(userDetails.getUsername() != null){
            if(request.isUserInRole("ROLE_STAFF")){
                String userId = userDetails.getUsername();

                //Send a list of the users courses to the model
                model.addAttribute("courseList", courseManagement.findByOwnerid(userId));
            }
        }

        //Return the home html template
        return "home";
    }


    /**
     * Landing url for the application when a user logs in. After authentication, redirect to home page
     * This is used so that users enter a directory where authentication is required, thus triggering shibboleth.
     * @return home.html template
     */
    @GetMapping("survey/home")
    public String loginHome() {
        return "redirect:/";
    }



    @GetMapping("/logout")
    public String logout(HttpSession session){
        session.invalidate();

        return "redirect:/";
    }

    @GetMapping("/privacy/download")
    public HttpEntity<byte[]> qrcode(HttpServletResponse response) throws IOException {


        // generate filename
        String filename = "static/resources/privacyPolicy.pdf";

        // Get PDF
        byte[] pdf = this.getClass().getClassLoader()
                .getResourceAsStream("static/resources/privacyPolicy.pdf").readAllBytes();


        // Set HTTP headers and return HttpEntity
        HttpHeaders header = new HttpHeaders();
        header.setContentType(MediaType.APPLICATION_PDF);
        header.set(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + filename);
        header.setContentLength(pdf.length);

        return new HttpEntity<>(pdf, header);
    }
}
