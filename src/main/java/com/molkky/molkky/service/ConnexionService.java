package com.molkky.molkky.service;


import com.molkky.molkky.domain.User;
import com.molkky.molkky.repository.UserRepository;
import lombok.Data;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.awt.*;

@Data
@Service
public class ConnexionService {
    @Autowired
    private UserRepository userRepository;
    }

