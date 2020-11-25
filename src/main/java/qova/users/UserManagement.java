package qova.users;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

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

//    public User findUserCreateIfNotPresent(String ajpPersistentId) {
//        Optional<User> user = userRepository.findByAjpPersistentId(ajpPersistentId);
//
//        if (user.isPresent()) {
//            return user.get();
//        } else {
//            User newUser = new User(ajpPersistentId, );
//            userRepository.save(newUser);
//            return newUser;
//        }
//    }


}
