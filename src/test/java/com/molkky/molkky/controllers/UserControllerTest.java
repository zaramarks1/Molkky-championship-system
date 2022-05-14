package com.molkky.molkky.controllers;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.test.web.servlet.MockMvc;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;


@WebMvcTest(value = UserController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class UserControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @Test
    void testUserController() throws Exception {
        mockMvc.perform(get("/addPlayer"))
                .andExpect(status().is4xxClientError());
    }
}
