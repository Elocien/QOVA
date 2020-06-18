package qova.admin;


import org.springframework.stereotype.Repository;
import org.springframework.data.repository.CrudRepository;

@Repository
public interface DefaultSurveyRepository extends CrudRepository <DefaultSurvey, Long> {

    static final Long specialId = 1L;

    default DefaultSurvey findSpecialInstance() {
        return findById(specialId).orElseThrow(() -> new IllegalStateException("No Default Survey Found"));
    }
}