package qova.users;

import org.hibernate.annotations.GenericGenerator;
import qova.enums.UserRoles;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import java.util.List;
import java.util.UUID;

@Entity
public class User {

    @Id
    @GeneratedValue(generator = "UUID")
    @GenericGenerator(name = "UUID", strategy = "org.hibernate.id.UUIDGenerator")
    @Column(name = "id", updatable = false, nullable = false, columnDefinition = "BINARY(16)")
    private UUID id;

    // The Id given by Shibboleth
    private final String ajpPersistentId;

    private final UserRoles userRole;

    public User(String ajpPersistentId, UserRoles userRole) {
        this.ajpPersistentId = ajpPersistentId;
        this.userRole = userRole;
    }

    public UUID getId() {
        return id;
    }

    public String getAjpPersistentId() {
        return ajpPersistentId;
    }

    public UserRoles getUserRole() {
        return this.userRole;
    }
}
