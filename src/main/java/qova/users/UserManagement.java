package qova.users;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;
import qova.objects.Course;

import javax.transaction.Transactional;
import java.util.Objects;
import java.util.Optional;

@Service
@Transactional
public class UserManagement {
    private final UserRepository userRepository;

    @Autowired
    public UserManagement(UserRepository userRepository) {
        this.userRepository = Objects.requireNonNull(userRepository);
    }

//    public String getUsername(UserDetails userDetails){
//        String usernameReduction = userDetails.getUsername();
//        return userDetails.getUsername().substring(usernameReduction.length() - 28);
//    }

    /**
     * Retrieves all {@linkplain User}s from the repository
     *
     * @return an {@linkplain Iterable} of all {@linkplain User}s
     */
    public Iterable<User> findAll() {
        return userRepository.findAll();
    }
}
