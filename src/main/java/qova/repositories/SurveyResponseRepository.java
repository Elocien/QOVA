package qova.repositories;

import org.springframework.stereotype.Repository;

import qova.objects.Course;
import qova.enums.CourseType;
import qova.objects.SurveyResponse;

import java.util.Optional;
import java.util.UUID;

import org.springframework.data.repository.CrudRepository;

@Repository
public interface SurveyResponseRepository extends CrudRepository <SurveyResponse, UUID> {
    
    Optional<SurveyResponse> findByCourseAndCourseTypeAndGroupNumberAndInstanceNumber(Course course, CourseType type, Integer groupNumber, Integer instanceNumber);

    Iterable<SurveyResponse> findByCourseAndCourseTypeAndInstanceNumber(Course course, CourseType type, Integer instanceNumber);

    Iterable<SurveyResponse> findByCourseAndCourseTypeAndGroupNumber(Course course, CourseType type, Integer groupNumber);

    Iterable<SurveyResponse> findByCourseAndCourseType(Course course, CourseType type);

    Iterable<SurveyResponse> findByCourse(Course course);
}
