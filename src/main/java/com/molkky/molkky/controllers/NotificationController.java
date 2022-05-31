package com.molkky.molkky.controllers;

import com.molkky.molkky.controllers.superclass.DefaultAttributes;
import com.molkky.molkky.exceptions.UnauthorizedException;
import com.molkky.molkky.model.SetModel;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import com.molkky.molkky.service.NotificationService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpSession;

@Controller
public class NotificationController extends DefaultAttributes {
    private static final Logger logger = LoggerFactory.getLogger(NotificationController.class);

    @Autowired
    private NotificationService notificationService;
    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;
//    mark all of user's notifications as read (on list opening)
    @PostMapping("/notifications/markAllAsRead")
    @ResponseBody
    public void markAllAsRead(HttpSession session, @ModelAttribute SetModel setModel) throws UnauthorizedException {
        UserLogged userLogged = session.getAttribute("user") != null ? (UserLogged) session.getAttribute("user") : null;
        if (userLogged != null) notificationService.markAllNotificationsAsRead(userTournamentRoleRepository.findById(userLogged.getTournamentRoleId()));
        else throw new UnauthorizedException("User not logged");
    }

    @ResponseStatus(value = HttpStatus.UNAUTHORIZED)
    @ExceptionHandler(UnauthorizedException.class)
    public void unauthorized() {
        logger.info("Unauthorized user");
    }

}
