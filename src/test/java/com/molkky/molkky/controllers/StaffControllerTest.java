package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.AddStaff;
import com.molkky.molkky.model.AddStaffList;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import com.molkky.molkky.service.EmailSenderService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
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

@WebMvcTest(value = StaffController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class StaffControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private StaffController staffController;

    @MockBean
    UserRepository userRepository;

    @MockBean
    TournamentRepository tournamentRepository;

    @MockBean
    EmailSenderService emailSenderService;

    @MockBean
    UserTournamentRoleRepository userTournamentRoleRepository;

    @Test
    void testAddStaff() throws Exception {
        int id = 89475;
        Tournament tournament = new Tournament();
        tournament.setId(id);

        List<AddStaff> mails = new ArrayList<>();
        AddStaffList staff = new AddStaffList();
        AddStaff addStaff = new AddStaff();
        addStaff.setTournamentId(89475);
        mails.add(addStaff);
        staff.setMails(mails);
        User mockedUser = mock(User.class);
        when(mockedUser.getPassword()).thenReturn("password");
        when(userRepository.findUserByEmail(anyString())).thenReturn(mockedUser);
        when(userRepository.save(any(User.class))).thenReturn(mockedUser);
        when(tournamentRepository.findById(89475)).thenReturn(tournament);
        mockMvc.perform(post("/staff/add")
                        .flashAttr("staff", staff))
                .andExpect(status().is3xxRedirection())
                .andExpect(view().name("redirect:/tournament/view?tournamentId=" + id));
    }

    @Test
    void testAddStaffWithoutUser() throws Exception {
        int id = 89475;
        String mail = "test@sfr.fr";
        Tournament tournament = new Tournament();
        tournament.setId(id);

        List<AddStaff> mails = new ArrayList<>();
        AddStaffList staff = new AddStaffList();
        AddStaff addStaff = new AddStaff();
        addStaff.setTournamentId(89475);
        addStaff.setMail(mail);
        mails.add(addStaff);
        staff.setMails(mails);

        User user = new User();
        user.setEmail(mail);

        when(tournamentRepository.findById(89475)).thenReturn(tournament);
        when(userRepository.existsUserByEmail(mail)).thenReturn(true);

        mockMvc.perform(post("/staff/add")
                        .flashAttr("staff", staff))
                .andExpect(status().is3xxRedirection())
                .andExpect(view().name("redirect:/tournament/view?tournamentId=" + id));
    }

    @Test
    void testAreAllDistinct(){
        String mail = "test@sfr.fr";

        AddStaffList staff = mock(AddStaffList.class);
        List<AddStaff> mails = new ArrayList<>();
        AddStaff addStaff = new AddStaff();
        addStaff.setTournamentId(89475);
        addStaff.setMail(mail);
        mails.add(addStaff);
        staff.setMails(mails);
        User mockedUser = mock(User.class);
        when(mockedUser.getPassword()).thenReturn("password");
        when(userRepository.findUserByEmail(anyString())).thenReturn(mockedUser);
        when(userRepository.save(any(User.class))).thenReturn(mockedUser);
        boolean result = staffController.areAllDistinct(staff);

        Assertions.assertTrue(result);
    }

}
