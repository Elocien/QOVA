package qova.responseTypes;

import org.springframework.stereotype.Repository;
import org.springframework.data.repository.CrudRepository;

@Repository
public interface MultipleChoiceResponseRepository extends CrudRepository <MultipleChoiceResponse, Long> {

}