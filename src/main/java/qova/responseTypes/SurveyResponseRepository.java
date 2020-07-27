package qova.responseTypes;

import qova.course.Course;
import qova.course.CourseType;


import org.springframework.stereotype.Repository;
import org.springframework.data.repository.CrudRepository;

@Repository
public interface SurveyResponseRepository extends CrudRepository <SurveyResponse, Long> {
    
    public Iterable<SurveyResponse> findByCourseAndCourseTypeAndClassNo(Course course, CourseType type, Integer classNo);

    public Iterable<SurveyResponse> findByCourseAndCourseType(Course course, CourseType type);

}
