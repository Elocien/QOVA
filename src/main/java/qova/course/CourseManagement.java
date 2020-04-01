package qova.course;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.Objects;


@Service
@Transactional
public class CourseManagement {
    
    // private final CourseRepository courses;
    private final CRUDCourseRepositoryTest coursesCRUD;
    
    @Autowired
    public CourseManagement(CRUDCourseRepositoryTest coursesCRUD){
        this.coursesCRUD = Objects.requireNonNull(coursesCRUD);
    }

    // public Course createCourse(){
    //     // Objects.requireNonNull(form);

    //     var name = "test";
    //     var type = CourseType.LECTURE;
    //     var survey = null;
    //     var classTotal = 2;
    //     var semester = 5;
    //     var faculty = CourseFaculty.COMPUTER_SCIENCE;



    //     return coursesCRUD.save(new Course());
    // }
   
}