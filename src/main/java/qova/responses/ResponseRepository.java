package qova.responses;

import qova.course.Course;
import qova.course.CourseType;

import java.util.Optional;

import org.springframework.stereotype.Repository;
import org.springframework.data.repository.CrudRepository;

@Repository
public interface ResponseRepository extends CrudRepository <Response, Long> {
    
    public Optional<Response> findByCourseAndCourseTypeAndClassNoAndPosition(Course course, CourseType type, Integer classNo, Integer position);

}
