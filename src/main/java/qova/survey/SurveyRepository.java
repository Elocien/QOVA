package qova.survey;

import qova.survey.Survey;

import org.springframework.stereotype.Repository;
import org.springframework.data.repository.CrudRepository;

@Repository
public interface SurveyRepository extends CrudRepository <Survey, Long> {
   
}
