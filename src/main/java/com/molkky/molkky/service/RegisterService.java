package com.molkky.molkky.service;

import com.molkky.molkky.domain.User;
import com.molkky.molkky.repository.UserRepository;
import lombok.Data;
import org.apache.commons.text.RandomStringGenerator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.List;


@Data
@Service
public class RegisterService {

    @Autowired
    private UserRepository userRepository;
    @Autowired
    private EmailSenderService senderService;

    public List<User> getAllUsers(){
        return userRepository.findAll();
    }

    public User saveUser(User user){
        userRepository.save(user);
        return user;
    }
    public void encodeAndSendEmail(User user){
        RandomStringGenerator pwdGenerator = new RandomStringGenerator.Builder().withinRange(33, 63)
                .build();
        String pwd = pwdGenerator.generate(10);
        senderService.sendEmail(user.getEmail(),"ton mdp","Tiens ton mdp: " + pwd);
        PasswordEncoder passwordEncoder = new BCryptPasswordEncoder();
        String hashedPassword = passwordEncoder.encode(pwd);
        user.setPassword(hashedPassword);
    }
}
