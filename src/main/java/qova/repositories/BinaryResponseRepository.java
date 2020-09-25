package qova.repositories;

import org.springframework.stereotype.Repository;

import qova.objects.BinaryResponse;
import qova.objects.SurveyResponse;

import java.util.Optional;

import org.springframework.data.repository.CrudRepository;

@Repository
public interface BinaryResponseRepository extends CrudRepository <BinaryResponse, Long> {
    
    public Optional<BinaryResponse> findBySurveyResponseAndSurveyPosition(SurveyResponse surveyResponse, Integer surveyPosition);

    public Iterable<BinaryResponse> findBySurveyResponse(SurveyResponse surveyResponse);

}