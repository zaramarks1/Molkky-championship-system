package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.User;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.service.EmailSenderService;
import com.molkky.molkky.service.NotificationService;
import com.molkky.molkky.service.RegisterService;
import com.molkky.molkky.service.UserService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(value = RegisterController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class RegisterControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private RegisterService registerService;

    @MockBean
    private TournamentRepository tournamentRepository;

    @MockBean
    private NotificationService notificationService;

    @MockBean
    private UserService userService;

    @MockBean
    private EmailSenderService senderService;

    @Test
    void testRegisterController() throws Exception {
        mockMvc.perform(get("/register/"))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("user"))
                .andExpect(view().name("/register"));

        mockMvc.perform(post("/saveUser/"))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/register"));

        verify(registerService, times(1)).encodeAndSendEmail(any(User.class));
        verify(registerService, times(1)).saveUser(any(User.class));
        verifyNoMoreInteractions(registerService);
    }
}
