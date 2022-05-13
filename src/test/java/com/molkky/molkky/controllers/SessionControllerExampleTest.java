package com.molkky.molkky.controllers;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.test.web.servlet.MockMvc;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(value = SessionControllerExample.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class SessionControllerExampleTest {

    @Autowired
    private MockMvc mockMvc;

    @Test
    void testSessionControllerExample() throws Exception {
        mockMvc.perform(get("/session"))
                .andExpect(status().isOk())
                .andExpect(view().name("/session"));

        mockMvc.perform(post("/persistMessage")
                        .param("msg", "Message test"))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/session"));

        mockMvc.perform(post("/destroy"))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/session"));

        mockMvc.perform(post("/goToRegister"))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/register"));
    }
}
