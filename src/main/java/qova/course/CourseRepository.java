package qova.course;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

@Repository
public class CourseRepository {
   
    @Autowired
    JdbcTemplate jdbcTemplate;

    public Course findById(long id) {

        return jdbcTemplate.queryForObject("select * from Course where id=?", new Object[] {
    
                id
    
            },
    
            new BeanPropertyRowMapper < Course > (Course.class));
    
    }
}


