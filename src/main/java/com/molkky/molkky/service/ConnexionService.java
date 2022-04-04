package com.molkky.molkky.service;


import com.molkky.molkky.domain.User;
import com.molkky.molkky.repository.UserRepository;
import lombok.Data;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;

@Data
@Service
public class ConnexionService {
    @Autowired
    private UserRepository userRepository;

    public void findByEmail(){
        userRepository.findById(1);
    }

    public boolean decode(String passwordNotEncrypted, User user) {
        boolean co =false;
        BCryptPasswordEncoder bcrypt = new BCryptPasswordEncoder();
        String userDBPassword = user.getCode();
        boolean isPasswordMatches = bcrypt.matches(passwordNotEncrypted, userDBPassword);
        if (isPasswordMatches) {
            System.out.println("Password Match");
            co = true;
        }
        return co;
    }
}