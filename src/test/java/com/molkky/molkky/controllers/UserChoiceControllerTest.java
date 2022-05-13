package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.User;
import com.molkky.molkky.domain.UserTournamentRole;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(value = UserChoiceController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class UserChoiceControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private UserRepository userRepository;

    @MockBean
    private UserTournamentRoleRepository userTournamentRoleRepository;

    @MockBean
    private TournamentRepository tournamentRepository;

    @MockBean
    private UserTournamentRole userTournamentRole;

    @Test
    void testUserChoiceControllerWithException() throws Exception {
        mockMvc.perform(get("/user_choice/choiceTournament"))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("tournaments"));
        mockMvc.perform(post("/user_choice/choiceTournament"))
                .andExpect(status().isOk());
    }

    @Test
    void testUserChoiceControllerWithoutException() throws Exception {

        mockMvc.perform(post("/user_choice/choiceTournament")
                        .param("tournamentId", "1"))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("roles"))
                .andExpect(view().name("/user_choice/choiceRole"));
    }

    @Test
    void testUserChoiceControllerChoose() throws Exception {

        UserTournamentRole userTournamentRole1 = new UserTournamentRole();
        User userChoice = new User();
        userChoice.setPseudo("TEST");

        userTournamentRole1.setUser(userChoice);

        when(this.userTournamentRoleRepository.findById(anyInt())).thenReturn(userTournamentRole1);

        mockMvc.perform(post("/user_choice/choiceRole")
                        .param("roleId", "1"))
                .andExpect(status().isOk())
                .andExpect(view().name("/home"));
    }
}
