package qova.responseTypes;

import qova.course.Course;
import qova.course.CourseType;

import org.springframework.stereotype.Repository;

import java.util.Optional;

import org.springframework.data.repository.CrudRepository;

@Repository
public interface SurveyResponseRepository extends CrudRepository <SurveyResponse, Long> {
    
    public Optional<SurveyResponse> findByCourseAndCourseTypeAndGroupNumberAndInstanceNumber(Course course, CourseType type, Integer groupNumber, Integer instanceNumber);

}
