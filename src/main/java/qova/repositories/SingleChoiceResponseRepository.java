package qova.repositories;

import org.springframework.stereotype.Repository;

import qova.objects.SingleChoiceResponse;
import qova.objects.SurveyResponse;

import java.util.Optional;

import org.springframework.data.repository.CrudRepository;

@Repository
public interface SingleChoiceResponseRepository extends CrudRepository <SingleChoiceResponse, Long> {
    
    public Optional<SingleChoiceResponse> findBySurveyResponseAndSurveyPosition(SurveyResponse surveyResponse, Integer surveyPosition);

    public Iterable<SingleChoiceResponse> findBySurveyResponse(SurveyResponse surveyResponse);

}