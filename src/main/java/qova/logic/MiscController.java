package qova.logic;

import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.util.ResourceUtils;
import org.springframework.web.bind.annotation.GetMapping;

import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

@Controller
public class MiscController {

    /**
     * Landing Page for the application
     * @return home.html template
     */
    @GetMapping("/")
    public String welcome() {
        return "home";
    }

    /**
     * Landing Page for the application, identical to {@linkplain MiscController#welcome()}
     * @return home.html template
     */
    @GetMapping("/home")
    public String home() {
        return "home";
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
