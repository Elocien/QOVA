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


    public Boolean setAdminUser(String userId){
        Optional<User> user = userRepository.findByAjpPersistentId(userId);
        if(user.isPresent()){
            user.get().setUserRole("ADMIN");
        }
        return false;
    }

    /**
     * Retrieves all {@linkplain User}s from the repository
     *
     * @return an {@linkplain Iterable} of all {@linkplain User}s
     */
    public Iterable<User> findAll() {
        return userRepository.findAll();
    }


    /**
     * Finds all {@linkplain User}s with the "ADMIN" role
     * @return Iterable of all admin users
     */
    public Iterable<User> findAdminUsers(){
        return userRepository.findByUserRole("ADMIN");
    }

    /**
     * Finds user with a given ID
     * @param userId User id in form of "https://idp.tu-dresden.de/idp/shibboleth!https://qova.med.tu-dresden.de/shibboleth!____________________________"
     * @return {@linkplain Optional} encapsulating a {@linkplain User}
     */
    public Optional<User> findById(String userId){
        return userRepository.findByAjpPersistentId(userId);
    }
}
