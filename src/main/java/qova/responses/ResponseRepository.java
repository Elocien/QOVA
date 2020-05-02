package qova.responses;


import org.springframework.stereotype.Repository;
import org.springframework.data.repository.CrudRepository;

@Repository
public interface ResponseRepository extends CrudRepository <Response, Long> {
    
}
