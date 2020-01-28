package qova.course;

import java.util.Optional;
import java.util.stream.Stream;

import org.springframework.data.domain.Sort;
import org.springframework.data.repository.CrudRepository;


public interface surveyRepository extends CrudRepository<course, Long> {
    
}