package qova.responses;

import qova.course.Course;
import qova.course.CourseType;


import org.springframework.stereotype.Repository;
import org.springframework.data.repository.CrudRepository;

@Repository
public interface ResponseRepository extends CrudRepository <Response, Long> {
    
    public Iterable<Response> findByCourseAndCourseTypeAndClassNo(Course course, CourseType type, Integer classNo);

    public Iterable<Response> findByCourseAndCourseType(Course course, CourseType type);

}
