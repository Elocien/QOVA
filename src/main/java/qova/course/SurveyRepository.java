package qova.course;

import org.springframework.data.repository.CrudRepository;


public interface SurveyRepository extends CrudRepository<Course, Long> {
    
}