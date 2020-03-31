package qova.course;

import qova.course.CourseManagement;


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