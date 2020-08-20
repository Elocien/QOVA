package qova.admin;


import org.springframework.stereotype.Repository;

import java.util.UUID;

import org.springframework.data.repository.CrudRepository;

@Repository
public interface DefaultSurveyRepository extends CrudRepository <DefaultSurvey, UUID> {

    public static final UUID specialId = UUID.fromString("123e4567-e89b-12d3-a456-556642440000");


    default DefaultSurvey findSpecialInstance() {
        return findById(specialId).orElseThrow(() -> new IllegalStateException("No Default Survey Found"));
    }
}