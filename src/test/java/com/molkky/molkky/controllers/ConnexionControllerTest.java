package com.molkky.molkky.controllers;


import com.molkky.molkky.domain.User;
import com.molkky.molkky.domain.UserTournamentRole;
import com.molkky.molkky.model.UserConnectionModel;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(value = ConnexionController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
public class ConnexionControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ConnexionController connexionController;

    @MockBean
    private UserRepository userRepository;

    @MockBean
    private UserTournamentRoleRepository userTournamentRoleRepository;

    @Mock
    private UserConnectionModel userConnectionModel;

    @Test
    public void testConnexionControllerWithPlayers() throws Exception {
        mockMvc.perform(get("/connexion/"))
                .andExpect(status().isOk())
                .andExpect(view().name("/connexion"));

        List<UserTournamentRole> players = new ArrayList<>();
        players.add(new UserTournamentRole());

        User user = new User();
        when(this.userRepository.findUserByEmailAndPassword("test72@sfr.fr", "testMDP")).thenReturn(user);

        when(this.userRepository.existsUserByEmailAndPassword(any(), any())).thenReturn(true);
        when(this.userTournamentRoleRepository.findUserWithCode(user, "code1")).thenReturn(players);

        mockMvc.perform(post("/connexion/")
                        .param("email", "test72@sfr.fr")
                        .param("password", "testMDP")
                        .param("code","code1")
                        .flashAttr("userConnection", new UserConnectionModel()))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/"));
    }

    @Test
    public void testConnexionControllerWith1AdminOrStaff() throws Exception{
        User user = new User();
        when(this.userRepository.findUserByEmailAndPassword("test72@sfr.fr", "testMDP")).thenReturn(user);

        List<UserTournamentRole> adminOrStaff = new ArrayList<>();
        adminOrStaff.add(new UserTournamentRole());

        when(this.userRepository.existsUserByEmailAndPassword(any(), any())).thenReturn(true);
        when(this.userTournamentRoleRepository.findUserAdminStaff(user)).thenReturn(adminOrStaff);

        mockMvc.perform(post("/connexion/")
                        .param("email", "test72@sfr.fr")
                        .param("password", "testMDP")
                        .flashAttr("userConnection", new UserConnectionModel()))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/"));
    }

    @Test
    public void testConnexionControllerWithAdminOrStaff() throws Exception{
        User user = new User();
        when(this.userRepository.findUserByEmailAndPassword("test72@sfr.fr", "testMDP")).thenReturn(user);

        List<UserTournamentRole> adminOrStaff = new ArrayList<>();
        adminOrStaff.add(new UserTournamentRole());
        adminOrStaff.add(new UserTournamentRole());

        when(this.userRepository.existsUserByEmailAndPassword(any(), any())).thenReturn(true);
        when(this.userTournamentRoleRepository.findUserAdminStaff(user)).thenReturn(adminOrStaff);

        mockMvc.perform(post("/connexion/")
                        .param("email", "test72@sfr.fr")
                        .param("password", "testMDP")
                        .flashAttr("userConnection", new UserConnectionModel()))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/user_choice/choiceTournament"));
    }

    @Test
    public void testConnexionControllerWithoutAdminOrStaff() throws Exception{
        User user = new User();
        when(this.userRepository.findUserByEmailAndPassword("test72@sfr.fr", "testMDP")).thenReturn(user);

        List<UserTournamentRole> adminOrStaff = new ArrayList<>();

        when(this.userRepository.existsUserByEmailAndPassword(any(), any())).thenReturn(true);
        when(this.userTournamentRoleRepository.findUserAdminStaff(user)).thenReturn(adminOrStaff);

        mockMvc.perform(post("/connexion/")
                        .param("email", "test72@sfr.fr")
                        .param("password", "testMDP")
                        .flashAttr("userConnection", new UserConnectionModel()))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/connexion"));
    }
}
