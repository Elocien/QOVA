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
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import qova.objects.Course;
import qova.users.User;
import qova.users.UserManagement;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Objects;
import java.util.UUID;

@Controller
public class MiscController {

    @Autowired
    private final CourseManagement courseManagement;

    @Autowired
    private final UserManagement userManagement;

    @Autowired
    MiscController(CourseManagement courseManagement, UserManagement userManagement){
        this.courseManagement = Objects.requireNonNull(courseManagement);
        this.userManagement = Objects.requireNonNull(userManagement);
    }

    /**
     * Landing Page for the application
     * @return home.html template
     */
    @GetMapping("/")
    public String welcome(Model model, @AuthenticationPrincipal UserDetails userDetails, HttpServletRequest request) {
        try {
            String userId = userDetails.getUsername();
            if(request.isUserInRole("ROLE_STAFF")){
                model.addAttribute("courseList", courseManagement.findByOwnerid(userId));
            }
        }
        catch(Exception e) {
            System.out.println(e);
        }
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



//    @GetMapping("fix")
//    public String fixUsernames(){
//        for (Course crs : courseManagement.findAll()){
//            String ownerId = crs.getOwnerId();
//            crs.setOwnerId(ownerId.substring(ownerId.length() - 28));
//        }
//
//        for(User usr : userManagement.findAll()){
//            String usrId = usr.getAjpPersistentId();
//            usr.setAjpPersistentId(usrId.substring(usrId.length() - 28));
//        }
//
//        return "solved";
//    }


}

