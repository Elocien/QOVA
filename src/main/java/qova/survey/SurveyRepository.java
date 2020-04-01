package qova.survey;


import org.springframework.stereotype.Repository;
import org.springframework.data.repository.CrudRepository;


@Repository
public interface SurveyRepository extends CrudRepository<Survey, Long> { 
   
    public Iterable<Survey> findAll();
    
    
}