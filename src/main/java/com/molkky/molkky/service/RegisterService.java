package com.molkky.molkky.service;

import com.molkky.molkky.domain.User;
import com.molkky.molkky.repository.UserRepository;
import lombok.Data;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;


@Data
@Service
public class RegisterService {

    @Autowired
    private UserRepository userRepository;

    public List<User> getAllUsers(){
        return userRepository.findAll();
    }

    public User saveUser(User user){
        userRepository.save(user);
        return user;
    }

}
