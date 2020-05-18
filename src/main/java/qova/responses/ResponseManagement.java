package qova.responses;

import java.time.LocalDateTime;
import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import qova.course.Course;

@Service
@Transactional
public class ResponseManagement {

    private final ResponseRepository responses;

    @Autowired
    public ResponseManagement(ResponseRepository responses) {
        this.responses = Objects.requireNonNull(responses);
    }

    
}