package com.molkky.molkky.service;


import com.molkky.molkky.domain.User;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;


@Service
public class ConnexionService {



    public boolean decode(String passwordNotEncrypted, User user) {
        boolean rightPassword = false;
        BCryptPasswordEncoder bcrypt = new BCryptPasswordEncoder();
        String userDBPassword = user.getPassword();
        boolean isPasswordMatches = bcrypt.matches(passwordNotEncrypted, userDBPassword);
        if (isPasswordMatches) {
            rightPassword = true;
        }
        return rightPassword;
    }
}

