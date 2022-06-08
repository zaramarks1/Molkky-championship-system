package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
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
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(value = DisplayUserController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class DisplayUserControllerTest {@Autowired
private MockMvc mockMvc;
    @MockBean
    private TeamRepository teamRepository;
    @MockBean
    private NotificationService notificationService;
    @MockBean
    private UserTournamentRoleRepository userTournamentRoleRepository;
    @MockBean
    private UserRepository userRepository;

    @Test
    void testGetUsers() throws Exception{
        User user = new User();
        List<User> users = new ArrayList();
        users.add(user);
        String name = "testUser";
        user.setSurname(name);
        //Mockito.when(Team.class.getName()).thenReturn(name);
        Mockito.when(userRepository.findAll()).thenReturn(users);
        Mockito.when(userRepository.searchUsersByName(Mockito.any(), Mockito.anyInt())).thenReturn(users);
        mockMvc.perform(get("/user/displayUsers"))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("users"))
                .andExpect(view().name("user/displayUsers"));
        Mockito.verify(userRepository, Mockito.times(1)).findAll();
        mockMvc.perform(get("/user/displayUsers?filter='bruh'"))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("users"))
                .andExpect(view().name("user/displayUsers"));
        Mockito.verify(userRepository, Mockito.times(1)).searchUsersByName(Mockito.any(), Mockito.anyInt());
    }
}
