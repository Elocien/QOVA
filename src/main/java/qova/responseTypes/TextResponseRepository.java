package qova.responseTypes;

import org.springframework.stereotype.Repository;
import org.springframework.data.repository.CrudRepository;

@Repository
public interface TextResponseRepository extends CrudRepository <TextResponse, Long> {

}