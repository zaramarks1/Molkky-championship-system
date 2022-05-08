package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Set;
import com.molkky.molkky.model.MatchModel;
import com.molkky.molkky.model.SetModel;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.model.UserModel;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.SetRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.service.MatchService;
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

import javax.servlet.http.HttpSession;

import static org.mockito.Mockito.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(value = SetController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class SetControllerTest {
    @Autowired
    private MockMvc mockMvc;
    @MockBean
    private MatchRepository matchRepository;
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
    void sendSetLogged() throws Exception {
//        given
        UserLogged userLogged = Mockito.mock(UserLogged.class);
        HttpSession session = new MockHttpSession(null, "user");
        session.setAttribute("user", userLogged);
        when(matchService.getUserTeamIndex(any(MatchModel.class), any(UserModel.class))).thenReturn(SetTeamIndex.TEAM1);
        Set set = new Set();
        set.setMatch(new Match());
        when(setService.getSetFromModel(any(SetModel.class))).thenReturn(set);
//        when
        mockMvc.perform(post("/sets/updateSet").sessionAttr("user", userLogged)).andExpect(status().isOk());
    }

}
