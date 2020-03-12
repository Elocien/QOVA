package qova.course;

import org.springframework.data.repository.CrudRepository;


public interface surveyRepository extends CrudRepository<Course, Long> {
    
}