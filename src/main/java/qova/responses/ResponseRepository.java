package qova.responses;

<<<<<<< HEAD
import qova.course.Course;
import qova.course.CourseType;

import java.util.Optional;
=======
>>>>>>> parent of 7182de3... Further additions to PDFGenerator class. Added Map of all responses for each position and started main logic for generation, based on ResponseType

import org.springframework.stereotype.Repository;
import org.springframework.data.repository.CrudRepository;

@Repository
public interface ResponseRepository extends CrudRepository <Response, Long> {
    
<<<<<<< HEAD
    public Optional<Response> findByCourseAndCourseTypeAndClassNoAndPosition(Course course, CourseType type, Integer classNo, Integer position);

=======
>>>>>>> parent of 7182de3... Further additions to PDFGenerator class. Added Map of all responses for each position and started main logic for generation, based on ResponseType
}
