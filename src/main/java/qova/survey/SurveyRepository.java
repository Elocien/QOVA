package qova.survey;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

@Repository
public class SurveyRepository {
   
    @Autowired
    JdbcTemplate jdbcTemplate;

    public Survey findById(long id) {

        return jdbcTemplate.queryForObject("select * from Survey where id=?", new Object[] {
    
                id
    
            },
    
            new BeanPropertyRowMapper < Survey > (Survey.class));
    
    }

    // public int insert(Survey survey) {

    //     return jdbcTemplate.update("insert into survey (id, questions) " + "values(?,  ?)",
    
    //         new Object[] {
    
    //             survey.getId(), survey.getQuestions()
    
    //         });
    
    // }
}
