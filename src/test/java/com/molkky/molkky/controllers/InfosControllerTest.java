package com.molkky.molkky.controllers;


import com.molkky.molkky.domain.User;
import com.molkky.molkky.domain.UserTournamentRole;
import com.molkky.molkky.model.CreateTeamModel;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.TournamentRepository;
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

import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockitoSession;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;

@WebMvcTest(value = InfosController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class InfosControllerTest {

    @Autowired
    private MockMvc mockMvc;
    @MockBean
    private NotificationService notificationService;
    @MockBean
    private UserRepository userRepository;
    @MockBean
    private UserTournamentRoleRepository userTournamentRoleRepository;

    @MockBean
    private MatchRepository matchRepository;

    @MockBean
    private TournamentRepository tournamentRepository;
    @Autowired
    private InfosController infosController;

    @Test
    void testInfosController() throws Exception {
        mockMvc.perform(get("/infos/"))
                .andExpect(status().isOk())
                .andExpect(view().name("infos"));
        UserLogged user = Mockito.mock(UserLogged.class);
        when(user.getId()).thenReturn(1);
        when(userRepository.findById(user.getId())).thenReturn(new User());
        mockMvc.perform(post("/changePseudo").sessionAttr("user", user)
                        .param("pseudo", "tweew"))
                .andExpect(view().name("redirect:/infos"));
        mockMvc.perform(post("/changeSurname").sessionAttr("user", user)
                        .param("surname", "test"))
                .andExpect(view().name("redirect:/infos"));
        mockMvc.perform(post("/changeForename").sessionAttr("user", user)
                        .param("forename", "test"))
                .andExpect(view().name("redirect:/infos"));
        mockMvc.perform(post("/changePassword").sessionAttr("user", user)
                        .param("pwd1", "test")
                        .param("pwd2","test"))
                .andExpect(view().name("redirect:/infos"));
        mockMvc.perform(post("/cancelRegisteration").sessionAttr("user", user))
                .andExpect(view().name("redirect:/connexion"));
    }
}
