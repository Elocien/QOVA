package qova.users;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import javax.transaction.Transactional;
import java.util.Objects;

@Service
@Transactional
public class UserManagement {
    private final UserRepository userRepository;

    @Autowired
    public UserManagement(UserRepository userRepository){
        this.userRepository = Objects.requireNonNull(userRepository);
    }


}
