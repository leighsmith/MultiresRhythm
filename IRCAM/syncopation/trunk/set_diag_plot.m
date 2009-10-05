function set_diag_plot(test_case)
%set_diag_plot Assigns the test_case into the plotting list.

    global plotting;
     
    % TODO Should test for set membership
    plotting = [plotting {test_case}];
end

