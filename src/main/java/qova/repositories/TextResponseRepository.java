package qova.repositories;

import org.springframework.stereotype.Repository;

import qova.objects.SurveyResponse;
import qova.objects.TextResponse;

import java.util.Optional;

import org.springframework.data.repository.CrudRepository;

@Repository
public interface TextResponseRepository extends CrudRepository <TextResponse, Long> {
    
    public Optional<TextResponse> findBySurveyResponseAndSurveyPosition(SurveyResponse surveyResponse, Integer surveyPosition);

    public Iterable<TextResponse> findBySurveyResponse(SurveyResponse surveyResponse);

}