package qova.repositories;

import org.springframework.stereotype.Repository;

import qova.objects.Course;
import qova.enums.CourseType;
import qova.objects.SurveyResponse;

import java.util.Optional;

import org.springframework.data.repository.CrudRepository;

@Repository
public interface SurveyResponseRepository extends CrudRepository <SurveyResponse, Long> {
    
    public Optional<SurveyResponse> findByCourseAndCourseTypeAndGroupNumberAndInstanceNumber(Course course, CourseType type, Integer groupNumber, Integer instanceNumber);

}
