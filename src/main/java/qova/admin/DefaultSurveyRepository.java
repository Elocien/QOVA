package qova.admin;


import org.springframework.stereotype.Repository;
import org.springframework.data.repository.CrudRepository;

@Repository
public interface DefaultSurveyRepository extends CrudRepository <DefaultSurvey, Long> {

}
