package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Court;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.AddCourt;
import com.molkky.molkky.model.AddCourtList;
import com.molkky.molkky.model.AddStaff;
import com.molkky.molkky.model.AddStaffList;
import com.molkky.molkky.repository.CourtRepository;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import org.junit.jupiter.api.Assertions;
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

import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;

@WebMvcTest(value = CourtController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class CourtControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private CourtController courtController;
    @MockBean
    private CourtRepository courtRepository;
    @MockBean
    UserRepository userRepository;

    @MockBean
    TournamentRepository tournamentRepository;

    @MockBean
    UserTournamentRoleRepository userTournamentRoleRepository;

    @Test
    void testAddCourt() throws Exception {
        int id = 89475;
        Tournament tournament = mock(Tournament.class);

        List<AddCourt> courts = new ArrayList<>();
        AddCourtList courtList = new AddCourtList();
        AddCourt addCourt = new AddCourt();
        addCourt.setTournamentId(id);
        addCourt.setName("Court 1");
        courts.add(addCourt);
        courtList.setCourts(courts);

        when(tournament.getCourts()).thenReturn(new ArrayList<>());
        when(tournament.getId()).thenReturn(id);
        when(tournamentRepository.findById(id)).thenReturn(tournament);
        mockMvc.perform(post("/court/add")
                        .flashAttr("courts", courtList))
                .andExpect(status().is3xxRedirection())
                .andExpect(view().name("redirect:/tournament/view?tournamentId=" + id));
        verify(courtRepository, times(1)).save(any(Court.class));
    }

}
