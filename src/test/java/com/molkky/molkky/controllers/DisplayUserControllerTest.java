package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Club;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.repository.*;
import com.molkky.molkky.service.NotificationService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

import java.util.ArrayList;
import java.util.List;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(value = DisplayUserController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class DisplayUserControllerTest {
    @Autowired
    private MockMvc mockMvc;
    @MockBean
    private TeamRepository teamRepository;
    @MockBean
    private UserRepository userRepository;
    @MockBean
    private TournamentRepository tournamentRepository;
    @MockBean
    private NotificationService notificationService;
    @MockBean
    private UserTournamentRoleRepository userTournamentRoleRepository;


    @Test
    void testGetUsers() throws Exception{
        mockMvc.perform(get("/user/displayUsers/"))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("users"))
                .andExpect(view().name("user/displayUsers"));
        User user = new User();
        List<User> users= new ArrayList();
        users.add(user);
        String pseudo = "testUser";
        user.setPseudo(pseudo);
    }

    @Test
    void testViewUser()throws Exception {
        User user = new User("pseudoTest", "surnameTest", "forenameTest", new Club(), "mailTest");
        user.setId(1);
        List<User> users = new ArrayList<>();
        users.add(user);
        Mockito.when(userRepository.findById(Mockito.anyInt())).thenReturn(user);
        mockMvc.perform(get("/user/view/")
                        .param("userId", user.getId().toString()))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("user"))
                .andExpect(model().attributeExists("tournament"))
                .andExpect(view().name("/user/displayDetailsUser"));
    }
}
