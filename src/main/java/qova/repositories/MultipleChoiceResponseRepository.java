package qova.repositories;

import org.springframework.stereotype.Repository;

import qova.objects.MultipleChoiceResponse;
import qova.objects.SurveyResponse;

import java.util.Optional;

import org.springframework.data.repository.CrudRepository;

@Repository
public interface MultipleChoiceResponseRepository extends CrudRepository <MultipleChoiceResponse, Long> {
    
    public Optional<MultipleChoiceResponse> findBySurveyResponseAndSurveyPosition(SurveyResponse surveyResponse, Integer surveyPosition);

    public Iterable<MultipleChoiceResponse> findBySurveyResponse(SurveyResponse surveyResponse);

}