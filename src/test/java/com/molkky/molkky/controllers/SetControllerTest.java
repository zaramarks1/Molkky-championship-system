package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Set;
import com.molkky.molkky.domain.UserTournamentRole;
import com.molkky.molkky.model.MatchModel;
import com.molkky.molkky.model.SetModel;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.model.UserTournamentRoleModel;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.SetRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import com.molkky.molkky.service.MatchService;
import com.molkky.molkky.service.NotificationService;
import com.molkky.molkky.service.SetService;
import com.molkky.molkky.service.UserService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.mock.web.MockHttpSession;
import org.springframework.test.web.servlet.MockMvc;
import type.SetTeamIndex;
import type.UserRole;

import javax.servlet.http.HttpSession;
import java.util.List;

import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrl;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(value = SetController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class SetControllerTest {
    @Autowired
    private MockMvc mockMvc;
    @MockBean
    private MatchRepository matchRepository;
    @MockBean
    private NotificationService notificationService;
    @MockBean
    private SetService setService;
    @MockBean
    private UserService userService;
    @MockBean
    private MatchService matchService;
    @MockBean
    private SetRepository setRepository;
    @MockBean
    private UserRepository userRepository;
    @MockBean
    private UserTournamentRoleRepository userTournamentRoleRepository;
    @Autowired
    private SetController setController;

    @Test
    void sendSetUnlogged() throws Exception {
//        given
//        when(matchService.getUserTeamIndex(any(MatchModel.class), any(UserModel.class))).thenReturn(SetTeamIndex.TEAM1);

        mockMvc.perform(post("/sets/updateSet"))
                .andExpect(status().is3xxRedirection());
    }

    @Test
    void sendSetLoggedTeam1() throws Exception {
//        given
        UserLogged userLogged = mock(UserLogged.class);
        userLogged.setTournamentRoleId(1);
        HttpSession session = new MockHttpSession(null, "user");
        session.setAttribute("user", userLogged);
        Set set = mock(Set.class);
        Match match = new Match();
        match.setId(1);
        match.setSets(List.of(set));

        UserTournamentRole userTournamentRole = Mockito.mock(UserTournamentRole.class);
        userTournamentRole.setId(1);
        userTournamentRole.setRole(UserRole.PLAYER);
        userTournamentRole.setIsRegistered(true);
        when(userTournamentRoleRepository.findById(anyInt())).thenReturn(userTournamentRole);

        when(set.getMatch()).thenReturn(match);
        when(setService.getSetFromModel(any(SetModel.class))).thenReturn(set);
        when(matchService.getUserTeamIndex(any(MatchModel.class), any(UserTournamentRoleModel.class))).thenReturn(SetTeamIndex.TEAM1);


//        when
        mockMvc.perform(post("/sets/updateSet").sessionAttr("user", userLogged))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/matches/match?match_id=1"));
//        then
        verify(setService, times(1)).enterSetResults(any(SetModel.class), any(UserTournamentRoleModel.class));
    }

    @Test
    void sendSetLoggedTeam2() throws Exception {
//        given
        UserLogged userLogged = mock(UserLogged.class);
        userLogged.setTournamentRoleId(1);
        HttpSession session = new MockHttpSession(null, "user");
        session.setAttribute("user", userLogged);
        Set set = mock(Set.class);
        Match match = new Match();
        match.setId(1);
        match.setSets(List.of(set));

        UserTournamentRole userTournamentRole = Mockito.mock(UserTournamentRole.class);
        userTournamentRole.setId(1);
        userTournamentRole.setRole(UserRole.PLAYER);
        userTournamentRole.setIsRegistered(true);
        when(userTournamentRoleRepository.findById(anyInt())).thenReturn(userTournamentRole);

        when(set.getMatch()).thenReturn(match);
        when(setService.getSetFromModel(any(SetModel.class))).thenReturn(set);
        when(matchService.getUserTeamIndex(any(MatchModel.class), any(UserTournamentRoleModel.class))).thenReturn(SetTeamIndex.TEAM2);


//        when
        mockMvc.perform(post("/sets/updateSet").sessionAttr("user", userLogged))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/matches/match?match_id=1"));
//        then
        verify(setService, times(1)).enterSetResults(any(SetModel.class), any(UserTournamentRoleModel.class));
    }

    @Test
    void sendSetLoggedOrga() throws Exception {
//        given
        UserLogged userLogged = mock(UserLogged.class);
        userLogged.setTournamentRoleId(1);
        HttpSession session = new MockHttpSession(null, "user");
        session.setAttribute("user", userLogged);
        Set set = mock(Set.class);
        Match match = new Match();
        match.setId(1);
        match.setSets(List.of(set));
        UserTournamentRole userTournamentRole = Mockito.mock(UserTournamentRole.class);
        userTournamentRole.setId(1);
        userTournamentRole.setRole(UserRole.PLAYER);
        userTournamentRole.setIsRegistered(true);
        when(userTournamentRoleRepository.findById(anyInt())).thenReturn(userTournamentRole);
        when(set.getMatch()).thenReturn(match);
        when(setService.getSetFromModel(any(SetModel.class))).thenReturn(set);
        when(matchService.getUserTeamIndex(any(MatchModel.class), any(UserTournamentRoleModel.class))).thenReturn(SetTeamIndex.ORGA);


//        when
        mockMvc.perform(post("/sets/updateSet").sessionAttr("user", userLogged))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/matches/match?match_id=1"));
//        then
        verify(setService, times(1)).enterSetResults(any(SetModel.class), any(UserTournamentRoleModel.class));
    }

}
