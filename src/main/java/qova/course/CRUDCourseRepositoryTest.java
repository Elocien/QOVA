package qova.course;


import org.springframework.stereotype.Repository;
import org.springframework.data.repository.CrudRepository;


@Repository
public interface CRUDCourseRepositoryTest extends CrudRepository<Course, Long> { 
   
    public Iterable<Course> findAll();
    
    
}


