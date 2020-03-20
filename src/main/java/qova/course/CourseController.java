package qova.course;


import lombok.Data;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@Controller
public class CourseController {

    @GetMapping("/")
    public String welcome (){
        return "home";
    }
}